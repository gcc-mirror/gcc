// { dg-do assemble  }
// GROUPS passed anonymous-unions
static union {
        char*   uC;
private:
        int     uI;// { dg-error "" } .*private member.*
};
