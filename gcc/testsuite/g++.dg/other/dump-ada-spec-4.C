/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

class Root  {
public:
   virtual void copyFrom(const Root& property) = 0;
};

class DT : Root {
   virtual void copyFrom (const Root& property);
   /* Although in C++ the type of argument Root is the parent type
      "Root", in order to properly override this primitive in Ada
      the profile of the generated function must be the derived
      type "DT" */
};

/* { dg-final { scan-ada-spec "Root'Class" } } */
/* { dg-final { cleanup-ada-spec } } */
