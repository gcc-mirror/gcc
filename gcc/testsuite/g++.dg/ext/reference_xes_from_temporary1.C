// PR c++/120529
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class Dst {};

class Src {
private:
    operator Dst() const;
};

class Src2 {
protected:
    operator Dst() const;
};

class Src3 {
public:
    operator Dst() const;
};

SA (!__reference_converts_from_temporary (Dst&&, Src));
SA (!__reference_constructs_from_temporary (Dst&&, Src));
SA (!__reference_converts_from_temporary (Dst&&, Src2));
SA (!__reference_constructs_from_temporary (Dst&&, Src2));
SA (__reference_converts_from_temporary (Dst&&, Src3));
SA (__reference_constructs_from_temporary (Dst&&, Src3));
