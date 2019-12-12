// PR c++/63558
// { dg-options "-fpermissive -w" }

extern int abs(int);
static long int n_ants;
enum enum_var_types
 { VAR_NONE, VAR_DELTA, VAR_SWITCH };

static enum enum_var_types option_var_n_ants;
void
adapt_parameters_next_iteration(void)
{
    switch(option_var_n_ants) {

    case VAR_NONE: break;

    case VAR_DELTA:
        int trunc_n_ants = 0;  // { dg-message "initialization" }
        n_ants += trunc_n_ants;
        break;
    case VAR_SWITCH:  // { dg-error "jump" }
        break;
      default: break;  // { dg-error "jump" }
    }
}
