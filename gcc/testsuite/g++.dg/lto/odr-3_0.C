// { dg-lto-do link }

typedef struct {
  int a; // { dg-lto-message "the first difference of corresponding definitions is field 'a'" }
} YYSTYPE; // { dg-lto-message "violates the C\\+\\+ One Definition Rule" 2 } 
  // Here we get warning and a note:
  // warning: type 'struct YYSTYPE' violates the C++ One Definition Rule
  // note: type 'struct YYSTYPE' itself violates the C++ One Definition Rule
union yyalloc { // { dg-lto-warning "7: type 'union yyalloc' violates the C\\+\\+ One Definition Rule" }
  short yyss;
  YYSTYPE yyvs; // { dg-lto-message "the first difference of corresponding definitions is field 'yyvs'" }
};
extern yyalloc a; // { dg-lto-warning "16: 'a' violates the C\\+\\+ One Definition Rule" }
int
main (void) {return a.yyss;}

/* Match warnings as follows:
  odr-3_0.C:5:3: warning: type 'struct YYSTYPE' violates the C++ One Definition Rule [-Wodr]
  odr-3_1.C:1:16: note: a different type is defined in another translation unit^
  odr-3_0.C:4:7: note: the first difference of corresponding definitions is field 'a'
  odr-3_1.C:1:16: note: a type with different number of fields is defined in another translation unit
  odr-3_0.C:9:7: warning: type 'union yyalloc' violates the C++ One Definition Rule [-Wodr]
  odr-3_1.C:6:7: note: a different type is defined in another translation unit
  odr-3_0.C:11:11: note: the first difference of corresponding definitions is field 'yyvs'
  odr-3_1.C:11:11: note: a field of same name but different type is defined in another translation unit
  odr-3_0.C:5:3: note: type 'struct YYSTYPE' itself violates the C++ One Definition Rule
  odr-3_0.C:13:16: warning: 'a' violates the C++ One Definition Rule [-Wodr]
  odr-3_1.C:6:7: note: type 'union yyalloc' itself violates the C++ One Definition Rule
  odr-3_0.C:9:7: note: the incompatible type is defined here
  odr-3_1.C:15:9: note: 'a' was previously declared here
  odr-3_1.C:15:9: note: code may be misoptimized unless -fno-strict-aliasing is used
*/
