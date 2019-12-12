// PR c++/86661

typedef int a;  // { dg-message "declared here" }
namespace {
class b {
  a c;
  template <typename> void a();  // { dg-error "changes meaning" }
};
}
