// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-additional-options "-fdiagnostics-set-output=text:experimental-nesting=yes,experimental-nesting-show-locations=no" }

struct dog{};
struct cat{};

void pet(dog);
void pet(cat);

template <class T>
concept pettable = requires(T t) { t.pet(); };

template <pettable T>
void pet(T);

struct donkey {};

int main() {
  pet(donkey{}); // { dg-error "no matching function for call to 'pet\\\(donkey\\\)'" }
}

/* { dg-begin-multiline-output "" }
  * note: there are 3 candidates
    * note: candidate 1: 'template<class T>  requires  pettable<T> void pet(T)'
      * note: template argument deduction/substitution failed:
        * note: constraints not satisfied
          * In substitution of 'template<class T>  requires  pettable<T> void pet(T) [with T = donkey]':
          * required from here
          * required for the satisfaction of 'pettable<T>' [with T = donkey]
          * in requirements with 'T t' [with T = donkey]
          * note: the required expression 't.pet()' is invalid
          * note: set '-fconcepts-diagnostics-depth=' to at least 2 for more detail
    * note: candidate 2: 'void pet(dog)'
      * note: no known conversion for argument 1 from 'donkey' to 'dog'
    * note: candidate 3: 'void pet(cat)'
      * note: no known conversion for argument 1 from 'donkey' to 'cat'
   { dg-end-multiline-output "" } */
