/* Test for -Wredundant-decls warnings */
/* { dg-do compile } */
/* { dg-options "-Wredundant-decls" } */

int j = 5; /* { dg-warning "previous" } */
int j;     /* { dg-warning "redundant" } */

static int k;
static int k = 5; /* { dg-warning "previous" } */
static int k;     /* { dg-warning "redundant" } */

static int l = 5; /* { dg-warning "previous" } */
static int l;     /* { dg-warning "redundant" } */

static int m;     /* { dg-warning "previous" } */
static int m;     /* { dg-warning "redundant" } */
static int m = 5;

int n;           /* { dg-warning "previous" } */
int n;           /* { dg-warning "redundant" } */
int n = 5; 

static int o;
static int o = 5;

int p;
int p = 5;
