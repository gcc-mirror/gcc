/* omp_interop_t is undefined (on purpose)  */


template<typename T, typename T2>
float repl1(T, T2, T2);  /* { dg-error "argument 2 of 'repl1' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(repl1) match(construct={dispatch}) append_args(interop(target,prefer_type(1,5,4)), interop(targetsync))  /* { dg-note "'append_args' specified here" }  */
template<typename T>
float base1(T);



template<typename T, typename T2, typename T3>
void repl3inval(T, T2, float);  /* { dg-error "argument 3 of 'repl3inval' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(repl3inval) match(construct={dispatch}) adjust_args(nothing : y) \
        append_args(interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")}),target,targetsync))  /* { dg-note "'append_args' specified here" }  */
template<typename T, typename T2>
void base2inval(T x, T2 y);



template<typename T>
void repl99(T);  /* { dg-error "argument 1 of 'repl99' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(repl99) match(construct={dispatch}) \
        append_args(interop(target, targetsync, prefer_type("cuda")))  /* { dg-note "'append_args' specified here" }  */
void base99();



template<typename T, typename T2, typename T3>
void repl2(T, T2, T3, T3);  /* { dg-error "argument 3 of 'repl2' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y) \
        append_args(interop(target, targetsync, prefer_type(1)),  /* { dg-note "'append_args' specified here" }  */ \
                    interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void base2(T x, T2 y);


template<typename T,typename T3>
void tooFewRepl(T, T, T3);  /* { dg-error "argument 3 of 'tooFewRepl' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(tooFewRepl) match(construct={dispatch}) \
        append_args(interop(target, targetsync, prefer_type(1)),  /* { dg-note "'append_args' specified here" }  */ \
                    interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void tooFewBase(T x, T2 y);



template<typename T, typename T2>
void repl3(T, T2, ...);  /* { dg-error "argument 2 of 'repl3' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(repl3) match(construct={dispatch}) \
        append_args(interop(prefer_type("cuda", "hsa")))  /* { dg-note "'append_args' specified here" }  */
template<typename T>
void base3(T, ...);
