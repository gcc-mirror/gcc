// PERMUTE_ARGS:

// $HeadURL$
// $Date$
// $Author$

// @author@     Anders F Björklund <afb@algonet.se>
// @date@       2005-01-25
// @uri@        news:ct428n$2qoe$1@digitaldaemon.com
// @url@        nntp://news.digitalmars.com/D.gnu/983

module run.unicode_06_哪里;

int 哪里(int ö){
        return ö+2;
}

int main(){
        assert(run.unicode_06_哪里.哪里(2)==4);
        return 0;
}
