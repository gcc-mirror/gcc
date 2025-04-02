import imports.test21098_phobos : Appender, Nullable;

struct Type {
    Nullable!(Type[]) templateArgs;
}

Type[] parseDeclarations() {
    Appender!(Type[]) members;
    return null;
}

enum ast = parseDeclarations();
