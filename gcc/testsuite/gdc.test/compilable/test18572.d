// https://issues.dlang.org/show_bug.cgi?id=18572

alias Seq(T...) = T;
void func(Seq!(int, int, int) args = Seq!(1, 2, 3)) {}
void func2(Seq!(int, int, int) args = Seq!(1,2,3), Seq!(int, int) args2 = Seq!(4, 5)) {}

void main(){
    Seq!(int,int,int) args = Seq!(1, 2, 3); // ok
    func(); // error
    func(args); // ok

    Seq!(int, int) args2 = Seq!(4, 5);
    func2();
    func2(args);
    func2(args, args2);
}
