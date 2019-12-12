module imports.ice10086y;

auto bind(alias f, bindValues...)()
{
    auto bind(Types...)(Types values)
    {
        return f(bindValues, values);
    }
    return bind();
}
