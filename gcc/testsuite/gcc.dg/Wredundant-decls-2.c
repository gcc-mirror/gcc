/* Test for -Wredundant-decls warnings */
/* { dg-do compile } */
/* { dg-options "-Wredundant-decls" } */

int j = 5; /* { dg-message "5:note: previous" } */
int j;     /* { dg-warning "5:redundant" } */

static int k;
static int k = 5; /* { dg-message "12:note: previous" } */
static int k;     /* { dg-warning "12:redundant" } */

static int l = 5; /* { dg-message "12:note: previous" } */
static int l;     /* { dg-warning "12:redundant" } */

static int m;     /* { dg-message "12:note: previous" } */
static int m;     /* { dg-warning "12:redundant" } */
static int m = 5;

int n;           /* { dg-message "5:note: previous" } */
int n;           /* { dg-warning "5:redundant" } */
int n = 5; 

static int o;
static int o = 5;

int p;
int p = 5;
