// { dg-do assemble  }
namespace a {
    class b {
	using std::c;  //{ dg-error "" } namespace using on class level
    };
}
