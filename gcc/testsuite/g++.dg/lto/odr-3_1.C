typedef struct YYSTYPE { // { dg-lto-message "type" 2 }
   // We get two notes here:
   // note: a different type is defined in another translation unit
   // note: a type with different number of fields is defined in another translation unit
} YYSTYPE;
union yyalloc { // { dg-lto-message "type" 2 }
   // We get here three notes:
   // note: a different type is defined in another translation unit
   // note: type 'union yyalloc' itself violates the C++ One Definition Rule
  short yyss;
  YYSTYPE yyvs; // { dg-lto-message "field of same name but different type is defined in another translation unit" }

}; 

yyalloc a;  // { dg-lto-message "'a' was previously declared here" }

