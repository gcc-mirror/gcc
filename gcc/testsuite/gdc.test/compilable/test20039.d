void foo()() { }
void bar(int) { }

alias bug = foo;
alias bug = bar;

template Identity(T...) { }

void main()
{
    alias member1 = Identity!(__traits(getMember, mixin(__MODULE__), "bug"));
    alias member2 = Identity!(__traits(getMember, mixin(__MODULE__), "bug"));
}
