// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-additional-options "-fdiagnostics-set-output=text:experimental-nesting=yes,experimental-nesting-show-locations=no" }
// { dg-additional-options "-fconcepts-diagnostics-depth=3" }

struct dog {};
struct cat {};

void pet(dog);
void pet(cat);

template <class T>
concept has_member_pet = requires(T t) { t.pet(); };

template <class T>
concept has_default_pet = T::is_pettable;

template <class T>
concept pettable = has_member_pet<T> or has_default_pet<T>;

void pet(pettable auto t);

struct lizard {};

int main() {
  pet(lizard{}); // { dg-error "no matching function for call to 'pet\\\(lizard\\\)'" }
}

/* { dg-begin-multiline-output "" }
  * there are 3 candidates
    * candidate 1: 'template<class auto:1>  requires  pettable<auto:1> void pet(auto:1)'
      * template argument deduction/substitution failed:
        * constraints not satisfied
          * In substitution of 'template<class auto:1>  requires  pettable<auto:1> void pet(auto:1) [with auto:1 = lizard]':
          * required from here
          * required for the satisfaction of 'pettable<auto:1>' [with auto:1 = lizard]
          * no operand of the disjunction is satisfied
            * the operand 'has_member_pet<T>' is unsatisfied because
              * required for the satisfaction of 'has_member_pet<T>' [with T = lizard]
              * required for the satisfaction of 'pettable<auto:1>' [with auto:1 = lizard]
              * in requirements with 'T t' [with T = lizard]
              * the required expression 't.pet()' is invalid, because
                * error: 'struct lizard' has no member named 'pet'
            * the operand 'has_default_pet<T>' is unsatisfied because
              * required for the satisfaction of 'has_default_pet<T>' [with T = lizard]
              * required for the satisfaction of 'pettable<auto:1>' [with auto:1 = lizard]
              * error: 'is_pettable' is not a member of 'lizard'
    * candidate 2: 'void pet(dog)'
      * no known conversion for argument 1 from 'lizard' to 'dog'
    * candidate 3: 'void pet(cat)'
      * no known conversion for argument 1 from 'lizard' to 'cat'
   { dg-end-multiline-output "" } */
