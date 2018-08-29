/* { dg-lto-do link } */
/* { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" } */

typedef struct _xmlDict xmlDict;
struct _xmlDict {
 int ref_counter;
};
void xmlDictCreate(void) {
  xmlDict * dict;
}

