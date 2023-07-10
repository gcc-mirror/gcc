module imports.issue23947a;

struct X { }
struct Y { }
class Class {
    private void handle(X x) { }
    public void handle(Y y) { }
}
