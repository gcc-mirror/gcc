// Build don't link: 
// GROUPS passed anonymous-unions
static union {
        char*   uC;
private:
        int     uI;// ERROR - .*private member.*
};
