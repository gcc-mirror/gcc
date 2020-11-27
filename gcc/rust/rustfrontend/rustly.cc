#include "rustly.h"
#include "node.h"

extern FILE* yyin;
static Rustly* rustly;


Rustly::Rustly (bool only_check_syntax, Linemap* linemap)
    : mSyntaxOnly(only_check_syntax),
      mLinemap(linemap)
{
    
}

Rustly::~Rustly ()
{
    
}

void Rustly::parse_input_files (size_t n, const char** in)
{
    int verbose = 1;
    parser_init(verbose);

    int ret = 0;
    
    size_t i;
    for (i = 0; i < n; ++i)
    {
        yyin = fopen(in[i], "rb");
        if (yyin == NULL) {
            fatal_error(UNKNOWN_LOCATION, "FAILED TO OPEN %s", in[i]);
            return;
        }

        ret = yyparse();        
    }


    printf("--- PARSE COMPLETE: ret:%d, n_nodes:%d ---\n", ret, n_nodes);
    if (nodes) {	
        print_node(nodes, 0);	
    }

    // FIXME double-free exists here
    // struct node *tmp;
    // while (nodes) {	
    //     tmp = nodes;
    //     nodes = tmp->next;	
    //     if (tmp->own_string) {	
    //         free((void*)tmp->name);
    //     }
    //     free(tmp);	
    // }

    if (mSyntaxOnly)
        return;


    // do type inferance

    // convert to GENERIC tree's
}


void Rustly::do_compile()
{
    // TODO
}

// C INTERFACE THIS NEEDS CLEANUP AT SOMEPOINT

void
rust_create_rustly(bool only_check_syntax, Linemap* linemap)
{
    rustly = new Rustly(only_check_syntax, linemap);
}

void
rust_parse_input_files (const char** in, unsigned int n)
{
    rustly->parse_input_files(n, in);
}
