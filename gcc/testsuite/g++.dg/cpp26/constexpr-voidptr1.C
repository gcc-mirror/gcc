// PR c++/110344
// { dg-do compile { target c++26 } }

#include <string_view>
struct Sheep {
  constexpr std::string_view speak() const noexcept { return "Baaaaaa"; }
};
struct Cow {
  constexpr std::string_view speak() const noexcept { return "Mooo"; }
};
class Animal_View {
private:
  const void *animal;
  std::string_view (*speak_function)(const void *);
public:
  template <typename Animal>
  constexpr Animal_View(const Animal &a)
    : animal{&a}, speak_function{[](const void *object) {
				   return static_cast<const Animal *>(object)->speak();
				 }} {}
  constexpr std::string_view speak() const noexcept {
    return speak_function(animal);
  }
};
// This is the key bit here. This is a single concrete function
// that can take anything that happens to have the "Animal_View"
// interface
constexpr std::string_view do_speak(Animal_View av) { return av.speak(); }
int main() {
  // A Cow is a cow. The only think that makes it special
  // is that it has a "std::string_view speak() const" member
  constexpr Cow cow;
  constexpr auto result = do_speak(cow);
  return static_cast<int>(result.size());
}
