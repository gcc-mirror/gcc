// { dg-do compile }
// { dg-additional-options "-fdump-lang-raw" }
// Check if dump file contains OBJ_TYPE_REF with additional fields (information about called virtual method).

class VExample {
public:
    virtual void methodV1() {}
    virtual void methodV2() {}
};

void funcA() {
    VExample objA;
    VExample *ptrA = &objA;

    ptrA->methodV2();
    ptrA->methodV1();
}

// { dg-final { scan-lang-dump-times {obj_type_ref[^\n]*type:} 2 raw } }
// { dg-final { scan-lang-dump-times {obj_type_ref[^\n]*expr:} 2 raw } }
// { dg-final { scan-lang-dump-times {obj_type_ref[^\n]*obj :} 2 raw } }
// { dg-final { scan-lang-dump-times {obj_type_ref[^\n]*\n[^\n]*tok :} 2 raw } }
