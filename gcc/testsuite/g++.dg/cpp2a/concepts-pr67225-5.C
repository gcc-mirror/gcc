// PR c++/67225
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template<typename A, typename T>
concept bool SomeConcept = true;

template <typename T>
void breaker(SomeConcept<int>);

class SomeClass {
    int privateMember;
};

int main() {
    return SomeClass().privateMember; // { dg-error "private within this context" }
}
