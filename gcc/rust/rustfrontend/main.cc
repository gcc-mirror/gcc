// would maybe have to implement lang-hooks.h methods:
/*  init_options: first call made to frontend before any options processing is done
    handle_option: called to handle a single command-line option
    post_options: called after all command-line processing is done. Could also determine name of 
    input file to parse here
    init: called after post_options to initalize frontend
    finish: called after all compilation is done. Can use to clean up front end.
    parse_file: lang hook that does the parsing, semantic analysis, and code generation required
    for input file, i.e. does the actual compilation work
*/


// reference links:
// https://www.linuxjournal.com/article/7884
