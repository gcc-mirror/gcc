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
  * there are 3 candidates
    * candidate 1: 'template<class T>  requires  pettable<T> void pet(T)'
      * template argument deduction/substitution failed:
        * constraints not satisfied
          * In substitution of 'template<class T>  requires  pettable<T> void pet(T) [with T = donkey]':
          * required from here
          * required for the satisfaction of 'pettable<T>' [with T = donkey]
          * in requirements with 'T t' [with T = donkey]
          * the required expression 't.pet()' is invalid
          * set '-fconcepts-diagnostics-depth=' to at least 2 for more detail
    * candidate 2: 'void pet(dog)'
      * no known conversion for argument 1 from 'donkey' to 'dog'
    * candidate 3: 'void pet(cat)'
      * no known conversion for argument 1 from 'donkey' to 'cat'
   { dg-end-multiline-output "" } */
