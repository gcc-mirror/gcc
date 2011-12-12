/* PR tree-optimization/50569 */
/* Reported by Paul Koning <pkoning@gcc.gnu.org> */
/* Reduced testcase by Mikael Pettersson <mikpe@it.uu.se> */

struct event {
    struct {
	unsigned int sec;
    } sent __attribute__((packed));
};

void __attribute__((noinline,noclone)) frob_entry(char *buf)
{
    struct event event;

    __builtin_memcpy(&event, buf, sizeof(event));
    if (event.sent.sec < 64) {
	event.sent.sec = -1U;
	__builtin_memcpy(buf, &event, sizeof(event));
    }
}

int main(void)
{
    union {
	char buf[1 + sizeof(struct event)];
	int align;
    } u;

    __builtin_memset(&u, 0, sizeof u);

    frob_entry(&u.buf[1]);

    return 0;
}
