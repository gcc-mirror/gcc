/* { dg-do "compile" } */
/* { dg-options "-std=gnu23" } */

typedef struct q { int x; } q_t;
typedef struct q { int x; } q_t;
typedef struct q { int x; } q_t;
typedef struct q { int x; } q_t;
typedef struct q { int x; } q_t;

typedef struct r r_t;
typedef struct r r_t;
typedef struct r r_t;
typedef struct r r_t;
typedef struct r r_t;

extern struct s { int x; } s;
extern struct s { int x; } s;
extern struct s { int x; } s;
extern struct s { int x; } s;
extern struct s { int x; } s;

struct t { int x; };
struct t { int x; };
struct t { int x; };
struct t { int x; };
struct t { int x; };

typedef enum e { E = 1 } e_t;
typedef enum e { E = 1 } e_t;
typedef enum e { E = 1 } e_t;
typedef enum e { E = 1 } e_t;
typedef enum e { E = 1 } e_t;

