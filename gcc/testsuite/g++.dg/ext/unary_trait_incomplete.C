// PR c++/39475

struct I; // { dg-message "forward declaration" }
struct C { };
union U; // { dg-message "forward declaration" }

bool nas1 = __has_nothrow_assign(I); // { dg-error "incomplete type" }
bool nas2 = __has_nothrow_assign(C[]);
bool nas3 = __has_nothrow_assign(I[]);
bool nas4 = __has_nothrow_assign(void);
bool nas5 = __has_nothrow_assign(const void);

bool tas1 = __has_trivial_assign(I); // { dg-error "incomplete type" }
bool tas2 = __has_trivial_assign(C[]);
bool tas3 = __has_trivial_assign(I[]);
bool tas4 = __has_trivial_assign(void);
bool tas5 = __has_trivial_assign(const void);

bool nco1 = __has_nothrow_constructor(I); // { dg-error "incomplete type" }
bool nco2 = __has_nothrow_constructor(C[]);
bool nco3 = __has_nothrow_constructor(I[]);
bool nco4 = __has_nothrow_constructor(void);
bool nco5 = __has_nothrow_constructor(const void);

bool tco1 = __has_trivial_constructor(I); // { dg-error "incomplete type" }
bool tco2 = __has_trivial_constructor(C[]);
bool tco3 = __has_trivial_constructor(I[]);
bool tco4 = __has_trivial_constructor(void);
bool tco5 = __has_trivial_constructor(const void);

bool ncp1 = __has_nothrow_copy(I); // { dg-error "incomplete type" }
bool ncp2 = __has_nothrow_copy(C[]);
bool ncp3 = __has_nothrow_copy(I[]);
bool ncp4 = __has_nothrow_copy(void);
bool ncp5 = __has_nothrow_copy(const void);

bool tcp1 = __has_trivial_copy(I); // { dg-error "incomplete type" }
bool tcp2 = __has_trivial_copy(C[]);
bool tcp3 = __has_trivial_copy(I[]);
bool tcp4 = __has_trivial_copy(void);
bool tcp5 = __has_trivial_copy(const void);

bool tde1 = __has_trivial_destructor(I); // { dg-error "incomplete type" }
bool tde2 = __has_trivial_destructor(C[]);
bool tde3 = __has_trivial_destructor(I[]);
bool tde4 = __has_trivial_destructor(void);
bool tde5 = __has_trivial_destructor(const void);

// T shall be a complete type, cv void, or an array of unknown bound.

bool con1 = __is_constructible(C);
bool con2 = __is_constructible(I); // { dg-error "incomplete type" }
bool con3 = __is_constructible(U); // { dg-error "incomplete type" }
bool con4 = __is_constructible(C[]);
bool con5 = __is_constructible(I[]);
bool con6 = __is_constructible(U[]);
bool con7 = __is_constructible(C[1]);
bool con8 = __is_constructible(I[1]); // { dg-error "incomplete type" }
bool con9 = __is_constructible(U[1]); // { dg-error "incomplete type" }
bool con10 = __is_constructible(void);
bool con11 = __is_constructible(const void);

// If T is a non-union class type, T shall be a complete type.

bool vde1 = __has_virtual_destructor(I); // { dg-error "incomplete type" }
bool vde2 = __has_virtual_destructor(U);
bool vde3 = __has_virtual_destructor(C[]);
bool vde4 = __has_virtual_destructor(I[]);
bool vde5 = __has_virtual_destructor(U[]);
bool vde6 = __has_virtual_destructor(C[1]);
bool vde7 = __has_virtual_destructor(I[1]);
bool vde8 = __has_virtual_destructor(U[1]);
bool vde9 = __has_virtual_destructor(void);
bool vde10 = __has_virtual_destructor(const void);

bool abs1 = __is_abstract(I); // { dg-error "incomplete type" }
bool abs2 = __is_abstract(U);
bool abs3 = __is_abstract(C[]);
bool abs4 = __is_abstract(I[]);
bool abs5 = __is_abstract(U[]);
bool abs6 = __is_abstract(C[1]);
bool abs7 = __is_abstract(I[1]);
bool abs8 = __is_abstract(U[1]);
bool abs9 = __is_abstract(void);
bool abs10 = __is_abstract(const void);

bool emp1 = __is_empty(I); // { dg-error "incomplete type" }
bool emp2 = __is_empty(U);
bool emp3 = __is_empty(C[]);
bool emp4 = __is_empty(I[]);
bool emp5 = __is_empty(U[]);
bool emp6 = __is_empty(C[1]);
bool emp7 = __is_empty(I[1]);
bool emp8 = __is_empty(U[1]);
bool emp9 = __is_empty(void);
bool emp10 = __is_empty(const void);

bool pol1 = __is_polymorphic(I); // { dg-error "incomplete type" }
bool pol2 = __is_polymorphic(U);
bool pol3 = __is_polymorphic(C[]);
bool pol4 = __is_polymorphic(I[]);
bool pol5 = __is_polymorphic(U[]);
bool pol6 = __is_polymorphic(C[1]);
bool pol7 = __is_polymorphic(I[1]);
bool pol8 = __is_polymorphic(U[1]);
bool pol9 = __is_polymorphic(void);
bool pol10 = __is_polymorphic(const void);

// If T is a class type, T shall be a complete type.

bool agg1 = __is_aggregate(I); // { dg-error "incomplete type" }
bool agg2 = __is_aggregate(U); // { dg-error "incomplete type" }
bool agg3 = __is_aggregate(C[]);
bool agg4 = __is_aggregate(I[]);
bool agg5 = __is_aggregate(U[]);
bool agg6 = __is_aggregate(C[1]);
bool agg7 = __is_aggregate(I[1]);
bool agg8 = __is_aggregate(U[1]);
bool agg9 = __is_aggregate(void);
bool agg10 = __is_aggregate(const void);

bool fin1 = __is_final(I); // { dg-error "incomplete type" }
bool fin2 = __is_final(U); // { dg-error "incomplete type" }
bool fin3 = __is_final(C[]);
bool fin4 = __is_final(I[]);
bool fin5 = __is_final(U[]);
bool fin6 = __is_final(C[1]);
bool fin7 = __is_final(I[1]);
bool fin8 = __is_final(U[1]);
bool fin9 = __is_final(void);
bool fin10 = __is_final(const void);

// remove_all_extents_t<T> shall be a complete type or cv void.

bool pod1 = __is_pod(I); // { dg-error "incomplete type" }
bool pod2 = __is_pod(U); // { dg-error "incomplete type" }
bool pod3 = __is_pod(C[]);
bool pod4 = __is_pod(I[]); // { dg-error "incomplete type" }
bool pod5 = __is_pod(U[]); // { dg-error "incomplete type" }
bool pod6 = __is_pod(C[1]);
bool pod7 = __is_pod(I[1]); // { dg-error "incomplete type" }
bool pod8 = __is_pod(U[1]); // { dg-error "incomplete type" }
bool pod9 = __is_pod(void);
bool pod10 = __is_pod(const void);
