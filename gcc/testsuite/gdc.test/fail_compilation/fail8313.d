auto bar()(int x){return x;}
auto bar()(int x = bar()){return x;}
static assert(bar(1));
