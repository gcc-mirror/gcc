/* Test to make sure that indirect jumps compile.  */
extern void bar(void);
void foo() { bar(); }
