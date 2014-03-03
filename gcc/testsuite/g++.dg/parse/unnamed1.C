// 9.4.2/4: Unnamed classes and classes contained directly or indirectly
// within unnamed classes shall not contain static data members.

typedef struct {		// { dg-message "unnamed" }
  static int i;			// { dg-error "static data member" }
} A;
