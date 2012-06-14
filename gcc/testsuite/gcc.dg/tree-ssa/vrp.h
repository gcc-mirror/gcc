extern void link_error(void);

#define RANGE(name, min, max) \
  if (name < min || name > max) \
    return;
#define ANTI_RANGE(name, min, max) \
  if (name >= min && name <= max) \
    return;
#define MERGE(cond, name1, name2) \
  if (cond) \
    name1 = name2;
#define CHECK_RANGE(expr, min, max) \
  do { \
     __typeof__ (expr) v = (expr); \
     if (v < min) link_error(); \
     if (v > max) link_error(); \
     if (v < min || v > max) link_error (); \
  } while (0) 
#define CHECK_ANTI_RANGE(expr, min, max) \
  do { \
    __typeof__ (expr) v = (expr); \
    if (v >= min) \
      if (v <= max) \
        link_error(); \
    if (v >= min && v <= max) \
      link_error(); \
  } while (0)
