/* { dg-do compile } */
/* { dg-options "" } */

#define CONCAT_(A,B) A ## B
#define CONCAT(A,B) CONCAT_(A,B)

#define ATTR __attribute__((AT))
#define ATSYM(suf) CONCAT (AT, CONCAT_ (_, suf))

#define AT noinline

typedef int ATSYM(type) ATTR; /* { dg-warning "attribute ignored" "" } */

typedef int (*ATSYM(fntype))(void) ATTR; /* { dg-warning "attribute ignored" "" } */

struct ATSYM(struct) {
  char dummy ATTR; /* { dg-warning "attribute ignored" "" } */
} ATTR; /* { dg-warning "does not apply to types" "" } */

int ATSYM(var) ATTR;  /* { dg-warning "attribute ignored" "" } */

int ATSYM(fn_knrarg) (arg)
  int arg ATTR; /* { dg-warning "attribute ignored" "" } */
{ return 0; }

int ATSYM(fn_isoarg) (int arg ATTR) { return 0; } /* { dg-warning "attribute ignored" "" } */

int ATSYM(fn_vars) (void) {
  static int svar ATTR; /* { dg-warning "attribute ignored" "" } */
  auto int lvar ATTR; /* { dg-warning "attribute ignored" "" } */
  return 0;
}


#undef AT
#define AT used

typedef int ATSYM(type) ATTR; /* used attribute is no longer
				 ignored.  */

typedef int (*ATSYM(fntype))(void) ATTR; /* used attribute is no
					    longer ignored.  */

struct ATSYM(struct) {
  char dummy ATTR; /* { dg-warning "attribute ignored" "" } */
} ATTR; /* { dg-warning "does not apply to types" "" } */

int ATSYM(var) ATTR;

int ATSYM(fn_knrarg) (arg)
  int arg ATTR; /* { dg-warning "attribute ignored" "" } */
{ return 0; }

int ATSYM(fn_isoarg) (int arg ATTR) { return 0; } /* { dg-warning "attribute ignored" "" } */

int ATSYM(fn_vars) (void) {
  static int svar ATTR;
  auto int lvar ATTR; /* { dg-warning "attribute ignored" "" } */
  return 0;
}


/* PR 28287 */
/* These are invalid on all targets.  Applying to PARM_ or FIELD_DECL
   also caused a tree checking ice on targets that support weak,  */
#undef AT
#define AT weak

typedef int ATSYM(type) ATTR; /* { dg-warning "attribute ignored" "" } */

typedef int (*ATSYM(fntype))(void) ATTR; /* { dg-warning "attribute ignored" "" } */

struct ATSYM(struct) {
  char dummy ATTR; /* { dg-warning "attribute ignored" "" } */
};

int ATSYM(fn_knrarg) (arg)
  int arg ATTR; /* { dg-warning "attribute ignored" "" } */
{ return 0; }

int ATSYM(fn_isoarg) (int arg ATTR) { return 0; } /* { dg-warning "attribute ignored" "" } */


/* PR target/28648  */
/* These are invalid on all targets.  Applying to PARM_ or FIELD_DECL
   also caused a tree checking ice on targets that support dllimport.  */
#undef AT
#define AT dllimport

typedef int ATSYM(type) ATTR;  /* { dg-warning "attribute |directive ignored" "" } */

typedef int (*ATSYM(fntype))(void) ATTR;   /* { dg-warning "attribute |directive ignored" "" } */

struct ATSYM(struct) {
  char dummy ATTR;  /* { dg-warning "attribute |directive ignored" "" } */
};

int ATSYM(fn_knrarg) (arg)
  int arg ATTR;  /* { dg-warning "attribute |directive ignored" "" } */
{ return 0; }

int ATSYM(fn_isoarg) (int arg ATTR) { return 0; }  /* { dg-warning "attribute |directive ignored" "" } */
