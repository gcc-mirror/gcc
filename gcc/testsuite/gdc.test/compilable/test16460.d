module imports.test16460;

void bug()
{
    auto d1 = (){
        import imports.imp16460;
        return val;
    };
    enum d2 = (){
        import imports.imp16460;
        return val;
    };
}
