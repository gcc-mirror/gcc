template <typename T> void specializable (T);

/* Invalid template: within "extern C".  */

extern "C" { // { dg-message "1: 'extern .C.' linkage started here" }

template <typename T> // { dg-error "template with C linkage" }
void within_extern_c_braces (void);

}

/* Valid template: not within "extern C".  */

template <typename T>
void not_within_extern_c (void);


/* Invalid specialization: within "extern C".  */

extern "C" { // { dg-message "1: 'extern .C.' linkage started here" }

template <>  // { dg-error "template specialization with C linkage" }
void specializable (int);

}


/* Valid specialization: not within "extern C".  */
template <>
void specializable (char);


/* Example of extern C without braces.  */

extern "C" template <typename T> // { dg-line open_extern_c_no_braces }
void within_extern_c_no_braces (void);
// { dg-error "12: template with C linkage" "" { target *-*-* } open_extern_c_no_braces }
// { dg-message "1: 'extern .C.' linkage started here" "" { target *-*-* } open_extern_c_no_braces }


/* Nested extern "C" specifications.
   We should report within the innermost extern "C" that's still open.  */

extern "C" {
  extern "C" { // { dg-line middle_open_extern_c }
    extern "C" {
    }

    template <typename T>  // { dg-error "template with C linkage" }
    void within_nested_extern_c (void);
    // { dg-message "3: 'extern .C.' linkage started here" "" { target *-*-* } middle_open_extern_c }

    extern "C++" {
      /* Valid template: within extern "C++".  */
      template <typename T>
      void within_nested_extern_cpp (void);

      extern "C" {  // { dg-line last_open_extern_c }
	/* Invalid template: within "extern C".  */
	template <typename T> // { dg-error "template with C linkage" }
	void within_extern_c_within_extern_cpp (void);
	// { dg-message "7: 'extern .C.' linkage started here" "" { target *-*-* } last_open_extern_c }	
      }
    }
  }
}
