// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh


alias void V;
alias double* P;

/// -1
struct Structure {
    public P variable;  /// 0
    V mNone(lazy P p) {}  /// 1
    pure nothrow V mPrefix(lazy P p) {}   /// 2
    V mSuffix(lazy P p) pure nothrow {}   /// 3
//  pure nothrow V mPrefixTemplate(T)(lazy P p, T[] t...) {}   /// 4
    V mSuffixTemplate(T)(lazy P p, T[] t...) pure nothrow {}   /// 5
    pure nothrow {
        V mScoped(lazy P p) {}    /// 6
    }
    pure nothrow auto mAutoPrefix(ref P p) { return p; } /// 7
//  pure nothrow auto mAutoTemplatePrefix(alias T)(ref T t) { return p; } /// 8
    auto mAutoTemplateSuffix(alias T)(ref T t) pure nothrow { return p; } /// 9
    pure nothrow:
    V mColon(lazy P p) {} /// 10
}

/// -1
class Class {
    public P variable;  /// 0
    V mNone(lazy P p) {}  /// 1
    pure nothrow V mPrefix(lazy P p) {}   /// 2
    V mSuffix(lazy P p) pure nothrow {}   /// 3
//  pure nothrow V mPrefixTemplate(T)(lazy P p, T[] t...) {}   /// 4
    V mSuffixTemplate(T)(lazy P p, T[] t...) pure nothrow {}   /// 5
    pure nothrow {
        V mScoped(lazy P p) {}    /// 6
    }
    pure nothrow auto mAutoPrefix(ref P p) { return p; } /// 7
//  pure nothrow auto mAutoTemplatePrefix(alias T)(ref T t) { return p; } /// 8
    auto mAutoTemplateSuffix(alias T)(ref T t) pure nothrow { return p; } /// 9
    pure nothrow:
    V mColon(lazy P p) {} /// 10
}

/+
/// -1
struct StructTemplate() {
    public P variable;  /// 0
    V mNone(lazy P p) {}  /// 1
    pure nothrow V mPrefix(lazy P p) {}   /// 2
    V mSuffix(lazy P p) pure nothrow {}   /// 3
//  pure nothrow V mPrefixTemplate(T)(lazy P p, T[] t...) {}   /// 4
    V mSuffixTemplate(T)(lazy P p, T[] t...) pure nothrow {}   /// 5
    pure nothrow {
        V mScoped(lazy P p) {}    /// 6
    }
    pure nothrow auto mAutoPrefix(ref P p) { return p; } /// 7
//  pure nothrow auto mAutoTemplatePrefix(alias T)(ref T t) { return p; } /// 8
    auto mAutoTemplateSuffix(alias T)(ref T t) pure nothrow { return p; } /// 9
    pure nothrow:
    V mColon(lazy P p) {} /// 10
}

/// -1
interface Interface {
    V mNone(lazy P p) ;  /// 1
    pure nothrow V mPrefix(lazy P p) ;   /// 2
    V mSuffix(lazy P p) pure nothrow ;   /// 3
//  pure nothrow V mPrefixTemplate(T)(lazy P p, T[] t...) ;   /// 4
    V mSuffixTemplate(T)(lazy P p, T[] t...) pure nothrow ;   /// 5
    pure nothrow {
        V mScoped(lazy P p) ;    /// 6
    }
//  pure nothrow auto mAutoTemplatePrefix(alias T)(ref T t) { return p; } /// 8
    auto mAutoTemplateSuffix(alias T)(ref T t) pure nothrow { return p; } /// 9
    pure nothrow:
    V mColon(lazy P p) ; /// 10
}
+/

public P variable;  /// 0
V mNone(lazy P p) {}  /// 1
pure nothrow V mPrefix(lazy P p) {}   /// 2
V mSuffix(lazy P p) pure nothrow {}   /// 3
//  pure nothrow V mPrefixTemplate(T)(lazy P p, T[] t...) {}   /// 4
V mSuffixTemplate(T)(lazy P p, T[] t...) pure nothrow {}   /// 5
pure nothrow {
    V mScoped(lazy P p) {}    /// 6
}
pure nothrow auto mAutoPrefix(ref P p) { return p; } /// 7
//  pure nothrow auto mAutoTemplatePrefix(alias T)(ref T t) { return p; } /// 8
auto mAutoTemplateSuffix(alias T)(ref T t) pure nothrow { return p; } /// 9
pure nothrow:
V mColon(lazy P p) {} /// 10
