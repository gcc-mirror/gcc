// Build don't link: 
// GROUPS passed parsing
class Try {
private:
  char s;
public:
  // an escaped double-quote should not call consume_string inside
  // reinit_parse_for_block
  void  mf() { s='\"'; }
};

int main()
{
  Try x;
  x.mf();
}

