// $HeadURL$
// $Date$
// $Author$

module dstress.run.named_entity_02;

// "-//W3C//ENTITIES Symbolic//EN//HTML"

int main(){
        assert('\&fnof;'==402);
        assert('\&Alpha;'==913);
        assert('\&Beta;'==914);
        assert('\&Gamma;'==915);
        assert('\&Delta;'==916);
        assert('\&Epsilon;'==917);
        assert('\&Zeta;'==918);
        assert('\&Eta;'==919);
        assert('\&Theta;'==920);
        assert('\&Iota;'==921);
        assert('\&Kappa;'==922);
        assert('\&Lambda;'==923);
        assert('\&Mu;'==924);
        assert('\&Nu;'==925);
        assert('\&Xi;'==926);
        assert('\&Omicron;'==927);
        assert('\&Pi;'==928);
        assert('\&Rho;'==929);
        assert('\&Sigma;'==931);
        assert('\&Tau;'==932);
        assert('\&Upsilon;'==933);
        assert('\&Phi;'==934);
        assert('\&Chi;'==935);
        assert('\&Psi;'==936);
        assert('\&Omega;'==937);
        assert('\&alpha;'==945);
        assert('\&beta;'==946);
        assert('\&gamma;'==947);
        assert('\&delta;'==948);
        assert('\&epsilon;'==949);
        assert('\&zeta;'==950);
        assert('\&eta;'==951);
        assert('\&theta;'==952);
        assert('\&iota;'==953);
        assert('\&kappa;'==954);
        assert('\&lambda;'==955);
        assert('\&mu;'==956);
        assert('\&nu;'==957);
        assert('\&xi;'==958);
        assert('\&omicron;'==959);
        assert('\&pi;'==960);
        assert('\&rho;'==961);
        assert('\&sigmaf;'==962);
        assert('\&sigma;'==963);
        assert('\&tau;'==964);
        assert('\&upsilon;'==965);
        assert('\&phi;'==966);
        assert('\&chi;'==967);
        assert('\&psi;'==968);
        assert('\&omega;'==969);
        assert('\&thetasym;'==977);
        assert('\&upsih;'==978);
        assert('\&piv;'==982);
        assert('\&bull;'==8226);
        assert('\&hellip;'==8230);
        assert('\&prime;'==8242);
        assert('\&Prime;'==8243);
        assert('\&oline;'==8254);
        assert('\&frasl;'==8260);
        assert('\&weierp;'==8472);
        assert('\&image;'==8465);
        assert('\&real;'==8476);
        assert('\&trade;'==8482);
        assert('\&alefsym;'==8501);
        assert('\&larr;'==8592);
        assert('\&uarr;'==8593);
        assert('\&rarr;'==8594);
        assert('\&darr;'==8595);
        assert('\&harr;'==8596);
        assert('\&crarr;'==8629);
        assert('\&lArr;'==8656);
        assert('\&uArr;'==8657);
        assert('\&rArr;'==8658);
        assert('\&dArr;'==8659);
        assert('\&hArr;'==8660);
        assert('\&forall;'==8704);
        assert('\&part;'==8706);
        assert('\&exist;'==8707);
        assert('\&empty;'==8709);
        assert('\&nabla;'==8711);
        assert('\&isin;'==8712);
        assert('\&notin;'==8713);
        assert('\&ni;'==8715);
        assert('\&prod;'==8719);
        assert('\&sum;'==8721);
        assert('\&minus;'==8722);
        assert('\&lowast;'==8727);
        assert('\&radic;'==8730);
        assert('\&prop;'==8733);
        assert('\&infin;'==8734);
        assert('\&ang;'==8736);
        assert('\&and;'==8743);
        assert('\&or;'==8744);
        assert('\&cap;'==8745);
        assert('\&cup;'==8746);
        assert('\&int;'==8747);
        assert('\&there4;'==8756);
        assert('\&sim;'==8764);
        assert('\&cong;'==8773);
        assert('\&asymp;'==8776);
        assert('\&ne;'==8800);
        assert('\&equiv;'==8801);
        assert('\&le;'==8804);
        assert('\&ge;'==8805);
        assert('\&sub;'==8834);
        assert('\&sup;'==8835);
        assert('\&nsub;'==8836);
        assert('\&sube;'==8838);
        assert('\&supe;'==8839);
        assert('\&oplus;'==8853);
        assert('\&otimes;'==8855);
        assert('\&perp;'==8869);
        assert('\&sdot;'==8901);
        assert('\&lceil;'==8968);
        assert('\&rceil;'==8969);
        assert('\&lfloor;'==8970);
        assert('\&rfloor;'==8971);
        //assert('\&lang;'==9001); // U+2329 valid for HTML 4.01; changed in HTML5
        //assert('\&rang;'==9002); // U+232A valid for HTML 4.01; changed in HTML5
    assert('\&lang;'==0x27E8); // valid for HTML 5 and later. The character was introduced in HTML 3.2
    assert('\&rang;'==0x27E9); // valid for HTML 5 and later. The character was introduced in HTML 3.2
        assert('\&loz;'==9674);
        assert('\&spades;'==9824);
        assert('\&clubs;'==9827);
        assert('\&hearts;'==9829);
        assert('\&diams;'==9830);
        return 0;
}

// Bug 5221
static assert('\&check;'==10003);
static assert('\&lsim;'==8818);
static assert('\&numero;'==8470);
static assert('\&urcorn;'==8989);
static assert('\&Zdot;'==379);
