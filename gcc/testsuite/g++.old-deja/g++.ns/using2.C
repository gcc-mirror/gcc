// Build don't link:
void f();
namespace A{
  using ::f;
}
