
class C
{
    this()
    {
        auto i = new class()
        {
            auto k = new class()
            {
                void func()
                {
                    this.outer.outer;
                }
            };
        };
    }
    int i;
}
void main() {}
