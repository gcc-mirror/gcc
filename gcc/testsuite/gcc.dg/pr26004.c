/* PR c/26004 */
/* Bug: the return slot optimization was taking the address of s_3,
   causing an error. */

struct s_3 { short s[3]; } z_3, s_3;
struct s_3 add_struct_3 (struct s_3 s){}
wack_struct_3 (void)
{
  int i; register struct s_3 u = z_3;
  u = add_struct_3 (u);
}
