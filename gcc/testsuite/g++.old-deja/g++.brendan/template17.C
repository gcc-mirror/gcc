// { dg-do assemble  }
// GROUPS passed templates
struct Regex { int i; Regex(char *, int); };
template<class KEY>
class NDAMName {
public:
    static const Regex pattern;
    static unsigned sequence_number;
};

const Regex NDAMName<'L'>::pattern("^[Ll](.*)$",   1);// { dg-error "type/value mismatch" "mismatch" }
// { dg-error "expected a type" "expected" { target *-*-* } 11 }
// { dg-warning "deprecated" "depr" { target *-*-* } 11 }
unsigned NDAMName<'L'>::sequence_number = 0;// { dg-error "type/value mismatch" "mismatch" }
// { dg-error "expected a type" "exp" { target *-*-* } 14 }
