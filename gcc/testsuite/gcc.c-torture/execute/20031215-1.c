/* PR middle-end/13400 */
/* The following test used to fail at run-time with a write to read-only
   memory, caused by if-conversion converting a conditional write into an
   unconditional write.  */

typedef struct {int c, l; char ch[3];} pstr;
const pstr ao = {2, 2, "OK"};
const pstr * const a = &ao;

void test1(void)
{
    if (a->ch[a->l]) {
        ((char *)a->ch)[a->l] = 0;
    }
}

void test2(void)
{
    if (a->ch[a->l]) {
        ((char *)a->ch)[a->l] = -1;
    }
}

void test3(void)
{
    if (a->ch[a->l]) {
        ((char *)a->ch)[a->l] = 1;
    }
}

int main(void)
{
    test1();
    test2();
    test3();
    return 0;
}

