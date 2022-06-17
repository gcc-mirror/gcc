

string classname(Object o)
{
    return typeid(o).name;
}

class Panzer {}
class Tiger : Panzer {}

static assert (() {
    Panzer p = new Tiger(); return classname(p);
} () == "Tiger");
