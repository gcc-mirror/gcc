// Postfix expression must be evaluated even if accessing a static member.

struct S
{
	static int i;
	S* foo();
};

S* S::foo(){
  i = 0;
  return this;
};

int S::i = 1;
int main(void)
{
	S * s = new S;
	int k=(s->foo())->i;
	return k;
}
