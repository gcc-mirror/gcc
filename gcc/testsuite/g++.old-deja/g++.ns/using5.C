// Build don't link:
namespace a {
    class b {
	using std::c;  //ERROR - namespace using on class level
    };
}
