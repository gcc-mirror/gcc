// PR c++/39475

struct I; // { dg-message "forward declaration" }
struct C { };

bool nas1 = __has_nothrow_assign(I); // { dg-error "incomplete type" }
bool nas2 = __has_nothrow_assign(C[]);
bool nas3 = __has_nothrow_assign(I[]); // { dg-error "incomplete type" }
bool nas4 = __has_nothrow_assign(void);
bool nas5 = __has_nothrow_assign(const void);

bool tas1 = __has_trivial_assign(I); // { dg-error "incomplete type" }
bool tas2 = __has_trivial_assign(C[]);
bool tas3 = __has_trivial_assign(I[]); // { dg-error "incomplete type" }
bool tas4 = __has_trivial_assign(void);
bool tas5 = __has_trivial_assign(const void);

bool nco1 = __has_nothrow_constructor(I); // { dg-error "incomplete type" }
bool nco2 = __has_nothrow_constructor(C[]);
bool nco3 = __has_nothrow_constructor(I[]); // { dg-error "incomplete type" }
bool nco4 = __has_nothrow_constructor(void);
bool nco5 = __has_nothrow_constructor(const void);

bool tco1 = __has_trivial_constructor(I); // { dg-error "incomplete type" }
bool tco2 = __has_trivial_constructor(C[]);
bool tco3 = __has_trivial_constructor(I[]); // { dg-error "incomplete type" }
bool tco4 = __has_trivial_constructor(void);
bool tco5 = __has_trivial_constructor(const void);

bool ncp1 = __has_nothrow_copy(I); // { dg-error "incomplete type" }
bool ncp2 = __has_nothrow_copy(C[]);
bool ncp3 = __has_nothrow_copy(I[]); // { dg-error "incomplete type" }
bool ncp4 = __has_nothrow_copy(void);
bool ncp5 = __has_nothrow_copy(const void);

bool tcp1 = __has_trivial_copy(I); // { dg-error "incomplete type" }
bool tcp2 = __has_trivial_copy(C[]);
bool tcp3 = __has_trivial_copy(I[]); // { dg-error "incomplete type" }
bool tcp4 = __has_trivial_copy(void);
bool tcp5 = __has_trivial_copy(const void);

bool vde1 = __has_virtual_destructor(I); // { dg-error "incomplete type" }
bool vde2 = __has_virtual_destructor(C[]);
bool vde3 = __has_virtual_destructor(I[]); // { dg-error "incomplete type" }
bool vde4 = __has_virtual_destructor(void);
bool vde5 = __has_virtual_destructor(const void);

bool tde1 = __has_trivial_destructor(I); // { dg-error "incomplete type" }
bool tde2 = __has_trivial_destructor(C[]);
bool tde3 = __has_trivial_destructor(I[]); // { dg-error "incomplete type" }
bool tde4 = __has_trivial_destructor(void);
bool tde5 = __has_trivial_destructor(const void);

bool abs1 = __is_abstract(I); // { dg-error "incomplete type" }
bool abs2 = __is_abstract(C[]);
bool abs3 = __is_abstract(I[]); // { dg-error "incomplete type" }
bool abs4 = __is_abstract(void);
bool abs5 = __is_abstract(const void);

bool pod1 = __is_pod(I); // { dg-error "incomplete type" }
bool pod2 = __is_pod(C[]);
bool pod3 = __is_pod(I[]); // { dg-error "incomplete type" }
bool pod4 = __is_pod(void);
bool pod5 = __is_pod(const void);

bool emp1 = __is_empty(I); // { dg-error "incomplete type" }
bool emp2 = __is_empty(C[]);
bool emp3 = __is_empty(I[]); // { dg-error "incomplete type" }
bool emp4 = __is_empty(void);
bool emp5 = __is_empty(const void);

bool pol1 = __is_polymorphic(I); // { dg-error "incomplete type" }
bool pol2 = __is_polymorphic(C[]);
bool pol3 = __is_polymorphic(I[]); // { dg-error "incomplete type" }
bool pol4 = __is_polymorphic(void);
bool pol5 = __is_polymorphic(const void);
