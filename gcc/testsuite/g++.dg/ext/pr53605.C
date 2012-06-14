// { dg-do compile }

// Avoid -pedantic-error default
// { dg-options "" }

template <bool lhs_is_null_literal>
class EqHelper {
public:
    template <typename T1, typename T2>
	static int  Compare( const T1& expected,
			     const T2& actual);
};
void foo(){
    static const int kData[] = {};
    ::EqHelper<false>::Compare(kData, "abc");
}
