/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int get_a_value ();
void printk(const char *);
void test_func()
{
    int loop;
    while (!loop) {             /* { dg-warning "is used uninitialized" } */
	loop = get_a_value();
	printk("...");
    }
}
