// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis of xobj member functions that have member function qualifiers.

struct S {
    void f_value_0(this S) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_value_1(this S) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_value_2(this S) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_value_3(this S) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_value_4(this S) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_value_5(this S) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_value_6(this S) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_value_7(this S) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_value_8(this S) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_value_9(this S) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_value_A(this S) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_ref_0(this S&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_ref_1(this S&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_ref_2(this S&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_ref_3(this S&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_ref_4(this S&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_ref_5(this S&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_ref_6(this S&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_ref_7(this S&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_ref_8(this S&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_ref_9(this S&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_ref_A(this S&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_refref_0(this S&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_refref_1(this S&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_refref_2(this S&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_refref_3(this S&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_refref_4(this S&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_refref_5(this S&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_refref_6(this S&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_refref_7(this S&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_refref_8(this S&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_refref_9(this S&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_refref_A(this S&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_cref_0(this S const&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cref_1(this S const&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cref_2(this S const&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cref_3(this S const&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cref_4(this S const&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cref_5(this S const&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cref_6(this S const&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cref_7(this S const&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cref_8(this S const&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cref_9(this S const&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cref_A(this S const&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_crefref_0(this S const&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_crefref_1(this S const&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_crefref_2(this S const&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_crefref_3(this S const&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_crefref_4(this S const&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_crefref_5(this S const&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_crefref_6(this S const&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_crefref_7(this S const&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_crefref_8(this S const&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_crefref_9(this S const&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_crefref_A(this S const&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_vref_0(this S volatile&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vref_1(this S volatile&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vref_2(this S volatile&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vref_3(this S volatile&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_vref_4(this S volatile&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_vref_5(this S volatile&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vref_6(this S volatile&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vref_7(this S volatile&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vref_8(this S volatile&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vref_9(this S volatile&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vref_A(this S volatile&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_vrefref_0(this S volatile&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vrefref_1(this S volatile&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vrefref_2(this S volatile&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_vrefref_3(this S volatile&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_vrefref_4(this S volatile&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_vrefref_5(this S volatile&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vrefref_6(this S volatile&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vrefref_7(this S volatile&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vrefref_8(this S volatile&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vrefref_9(this S volatile&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_vrefref_A(this S volatile&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_cvref_0(this S const volatile&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvref_1(this S const volatile&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvref_2(this S const volatile&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvref_3(this S const volatile&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cvref_4(this S const volatile&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cvref_5(this S const volatile&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvref_6(this S const volatile&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvref_7(this S const volatile&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvref_8(this S const volatile&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvref_9(this S const volatile&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvref_A(this S const volatile&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void f_cvrefref_0(this S const volatile&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvrefref_1(this S const volatile&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvrefref_2(this S const volatile&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void f_cvrefref_3(this S const volatile&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cvrefref_4(this S const volatile&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void f_cvrefref_5(this S const volatile&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvrefref_6(this S const volatile&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvrefref_7(this S const volatile&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvrefref_8(this S const volatile&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvrefref_9(this S const volatile&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void f_cvrefref_A(this S const volatile&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    template<typename Self> void d_templ_0(this Self&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    template<typename Self> void d_templ_1(this Self&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    template<typename Self> void d_templ_2(this Self&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    template<typename Self> void d_templ_3(this Self&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    template<typename Self> void d_templ_4(this Self&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    template<typename Self> void d_templ_5(this Self&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    template<typename Self> void d_templ_6(this Self&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    template<typename Self> void d_templ_7(this Self&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    template<typename Self> void d_templ_8(this Self&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    template<typename Self> void d_templ_9(this Self&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    template<typename Self> void d_templ_A(this Self&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }

    void d_auto_0(this auto&&) const;             // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void d_auto_1(this auto&&) volatile;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void d_auto_2(this auto&&) const volatile;    // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have cv-qualifier" }
    void d_auto_3(this auto&&) &;                 // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void d_auto_4(this auto&&) &&;                // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have ref-qualifier" }
    void d_auto_5(this auto&&) const &;           // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void d_auto_6(this auto&&) const &&;          // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void d_auto_7(this auto&&) volatile &;        // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void d_auto_8(this auto&&) volatile &&;       // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void d_auto_9(this auto&&) const volatile &;  // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
    void d_auto_A(this auto&&) const volatile &&; // { dg-error "explicit object member function '(?!static)\[^\n\r\]+' cannot have (ref|cv)-qualifier" }
};

