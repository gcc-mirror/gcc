typedef struct _xmlDict xmlDict;
struct _xmlDoc {
  struct _xmlDict *dict;
};
void xmlAddEntity(struct _xmlDoc *a) {
  xmlDict * dict = a->dict;
  xmlHashCreateDict(0, dict);
}

