// Build don't link: 
// GROUPS passed old-abort
typedef int _sigset_t;
extern "C" {
        extern int sigaction(int signo, const struct sigaction *action_spec_p, struct sigaction *old_action_p);
}
extern "C" {
        extern void foo();
};
class SS {
        friend void foo();
protected:
        void goo();
};
inline void
SS::goo() { }
