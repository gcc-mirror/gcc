// Build don't link: 
// GROUPS passed old-abort
typedef void (**ppfn)(void);

int main() {
    ppfn fn;

    fn = new (void(*)(void));

    return 0;
}
