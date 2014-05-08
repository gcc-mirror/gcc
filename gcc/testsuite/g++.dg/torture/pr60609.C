/* { dg-do assemble } */

class exception
{
};
class bad_alloc:exception
{
};
class logic_error:exception
{
};
class domain_error:logic_error
{
};
class invalid_argument:logic_error
{
};
class length_error:logic_error
{
};
class overflow_error:exception
{
};
typedef int mpz_t[];
template < class > class __gmp_expr;
template <> class __gmp_expr < mpz_t >
{
    ~__gmp_expr ();
};

class PIP_Solution_Node;
class internal_exception
{
    ~internal_exception ();
};
class not_an_integer:internal_exception
{
};
class not_a_variable:internal_exception
{
};
class not_an_optimization_mode:internal_exception
{
};
class not_a_bounded_integer_type_width:internal_exception
{
};
class not_a_bounded_integer_type_representation:internal_exception
{
};
class not_a_bounded_integer_type_overflow:internal_exception
{
};
class not_a_complexity_class:internal_exception
{
};
class not_a_control_parameter_name:internal_exception
{
};
class not_a_control_parameter_value:internal_exception
{
};
class not_a_pip_problem_control_parameter_name:internal_exception
{
};
class not_a_pip_problem_control_parameter_value:internal_exception
{
};
class not_a_relation:internal_exception
{
};
class ppl_handle_mismatch:internal_exception
{
};
class timeout_exception
{
    ~timeout_exception ();
};
class deterministic_timeout_exception:timeout_exception
{
};
void __assert_fail (const char *, const char *, int, int *)
__attribute__ ((__noreturn__));
void PL_get_pointer (void *);
int Prolog_is_address ();
inline int
Prolog_get_address (void **p1)
{
    Prolog_is_address ()? static_cast <
    void >(0) : __assert_fail ("Prolog_is_address", "./swi_cfli.hh", 0, 0);
    PL_get_pointer (p1);
    return 0;
}

class non_linear:internal_exception
{
};
class not_unsigned_integer:internal_exception
{
};
class not_universe_or_empty:internal_exception
{
};
class not_a_nil_terminated_list:internal_exception
{
};
class PPL_integer_out_of_range
{
    __gmp_expr < mpz_t > n;
};
void handle_exception ();
template < typename T > T * term_to_handle (int, const char *)
{
    if (Prolog_is_address ())
    {
        void *p;
        Prolog_get_address (&p);
        return static_cast < T * >(0);
    }
    throw;
}

void
ppl_new_MIP_Problem_from_MIP_Problem ()
try
{
    term_to_handle < int >(0, "ppl_new_MIP_Problem_from_MIP_Problem/2");
}

catch (exception &)
{
}

int
ppl_PIP_Tree_Node_parametric_values ()
{
    try
    {
        PIP_Solution_Node *a = term_to_handle < PIP_Solution_Node > (0, 0);
	(void)a;
        return 1;
    }
    catch (internal_exception &)
    {
    }
    catch (not_unsigned_integer &)
    {
        handle_exception ();
    }
    catch (non_linear &)
    {
        handle_exception ();
    }
    catch (not_a_variable &)
    {
        handle_exception ();
    }
    catch (not_an_integer &)
    {
        handle_exception ();
    }
    catch (ppl_handle_mismatch &)
    {
        handle_exception ();
    }
    catch (not_an_optimization_mode &)
    {
        handle_exception ();
    }
    catch (not_a_complexity_class &)
    {
        handle_exception ();
    }
    catch (not_a_bounded_integer_type_width &)
    {
        handle_exception ();
    }
    catch (not_a_bounded_integer_type_representation &)
    {
        handle_exception ();
    }
    catch (not_a_bounded_integer_type_overflow &)
    {
        handle_exception ();
    }
    catch (not_a_control_parameter_name &)
    {
        handle_exception ();
    }
    catch (not_a_control_parameter_value &)
    {
        handle_exception ();
    }
    catch (not_a_pip_problem_control_parameter_name &)
    {
        handle_exception ();
    }
    catch (not_a_pip_problem_control_parameter_value &)
    {
        handle_exception ();
    }
    catch (not_universe_or_empty &)
    {
        handle_exception ();
    }
    catch (not_a_relation &)
    {
        handle_exception ();
    }
    catch (not_a_nil_terminated_list &)
    {
        handle_exception ();
    }
    catch (PPL_integer_out_of_range &)
    {
        handle_exception ();
    }
    catch (int &)
    {
    } catch (timeout_exception &)
    {
        handle_exception ();
    } catch (deterministic_timeout_exception &)
    {
        handle_exception ();
    } catch (overflow_error &)
    {
        handle_exception ();
    } catch (domain_error &)
    {
        handle_exception ();
    } catch (length_error &)
    {
        handle_exception ();
    } catch (invalid_argument &)
    {
        handle_exception ();
    } catch (logic_error &)
    {
        handle_exception ();
    } catch (bad_alloc &)
    {
        handle_exception ();
    } catch (exception &)
    {
        handle_exception ();
    } catch ( ...)
    {
        handle_exception ();
    }
    return 0;
}
