// PR c++/82506
// { dg-do preprocess { target c++11 } }

#define STRINGIZE(A) #A

BEGIN STRINGIZE(R"(
)") END

// { dg-final { scan-file string-3.ii "BEGIN \"R\\\"(\\n)\\\"\"\n END" } }
