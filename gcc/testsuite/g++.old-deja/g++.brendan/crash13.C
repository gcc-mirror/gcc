// Build don't link: 
// GROUPS passed old-abort
// Special g++ Options: -Wreturn-type
class gen_op
{
public:
  gen_op ( );
  gen_op (const gen_op &Op1);
  ~gen_op ( );
  void operator = (const gen_op &Op1);
};




class spin_op 
{
public:
  spin_op();
  spin_op(const spin_op& SOp);
  ~spin_op();
  void operator= (const spin_op& SOp);
  operator gen_op();
};


spin_op Fe();


gen_op Spul_U_axis()
{
  gen_op U1;
  U1 = Fe();
}; // ERROR - reaches end of non-void function

int
main () {};
