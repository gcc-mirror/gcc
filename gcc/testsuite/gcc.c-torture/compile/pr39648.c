void
yysyntax_error (char *yyp)
{
        char const *yyf;
        char yyformat[5];

        yyf = yyformat;
        while ((*yyp = *yyf) != '\0') {
                if (yyf[1] == 's')
                        yyf += 2;
        }
}
