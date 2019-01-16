struct Object{}
class Game {}

void main()
{
    static assert(is(Object == struct));
    static assert(is(object.Object == class));
}
