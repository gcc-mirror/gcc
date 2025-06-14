/* { dg-do compile } */

struct T { int val; };

void f_int (int);
char* get_pos (void);
struct T* get_pT (void);

void func (char i)
{
    struct T t = * get_pT ();
    unsigned diff = get_pos () - &i;

    if (diff)
    {
        long val32 = t.val;
        if (get_pos ())
            val32 = diff;
        if (get_pos ())
            f_int (2 * val32);
    }
}
