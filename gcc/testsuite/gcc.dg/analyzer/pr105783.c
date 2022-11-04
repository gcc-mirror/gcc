/* { dg-additional-options "-O" } */

struct ss_s {
    union out_or_counting_u {
    	char *newstr;
    	unsigned long long cnt;
    } uu;
    _Bool counting;
};

struct ss_s ss_init(void) {
   struct ss_s rr = { .counting = 1 };
   return rr;
}

void ss_out(struct ss_s *t, char cc) {
   if (!t->counting) {
       *t->uu.newstr++ = cc;
   }
}

int main() {
    struct ss_s ss = ss_init();
    ss_out(&ss, 'a');
}

