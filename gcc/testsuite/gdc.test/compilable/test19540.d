alias inst = templ!();
template templ(T = typeof(new class {})) {}
