// 3737

int crayon;

struct SIB(alias junk)
{
    template Alike(V) {
        enum bool Alike = Q == V.garbage;
    }
    void opDispatch(string s)() {
        static assert(Alike!(SIB!(crayon)));
    }
}

void main() {
      SIB!(SIB!(crayon).E)(3.0);
}
