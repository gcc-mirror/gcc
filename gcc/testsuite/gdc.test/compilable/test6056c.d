alias int T;
static assert( is( T** : const(T**) ));
static assert( is( T*  : const(T* ) ));
