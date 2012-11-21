/* { dg-do compile } */
/* { dg-add-options bind_pic_locally } */

struct B
{
    virtual void test_suite_finish ();
};
void
fn1 (B & p2)
{
    fn1 (p2);
    B & a = p2;
    a.test_suite_finish ();
    B b;
    fn1 (b);
}
