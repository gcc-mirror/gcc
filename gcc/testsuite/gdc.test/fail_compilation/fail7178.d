template populate(overloads...)
{
    mixin populate!(.contents);
}
public mixin populate!int;

