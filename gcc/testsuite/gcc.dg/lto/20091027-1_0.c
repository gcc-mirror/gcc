/* { dg-lto-do link } */
/* { dg-extra-ld-options "-r -nostdlib" } */

typedef struct _xmlDict xmlDict;
struct _xmlDict {
 int ref_counter;
};
void xmlDictCreate(void) {
  xmlDict * dict;
}

