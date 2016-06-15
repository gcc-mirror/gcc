/* PR71529 */
/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

class c1
{
 public:
  virtual ~c1 ();
};

class c2
{
 public:
  virtual ~c2 ();
};

class c3 : c1, c2 { };

int main (int, char **)
{
  c3 obj;
}
