/* PR middle-end/52306 */

struct xmlNs {
    const unsigned char *prefix;
};

struct xmlNode {
    const unsigned char *name;
    struct xmlNs *ns;
    struct xmlNs *nsDef;
};

struct xsltTemplate {
    const unsigned char *name;
    int inheritedNsNr;
    void *inheritedNs;
};

struct xsltTemplate *xsltNewTemplate(void);
void xsltGetQNameURI(unsigned char**);
long xmlMalloc(unsigned long);
void xsltGenericDebug(void);
int xmlStrEqual(const unsigned char*, const unsigned char*);

static void xsltGetInheritedNsList(
    struct xsltTemplate *template,
    struct xmlNode *node)
{
    struct xmlNs *cur;
    struct xmlNs **ret;
    int nbns = 0;
    int maxns = 10;
    int i;

    if (template == 0
	|| template->inheritedNsNr != 0
	|| template->inheritedNs != 0)
	return;

    while (node != 0) {
	cur = node->nsDef;
	ret = (struct xmlNs**) xmlMalloc((maxns + 1) * sizeof(struct xmlNs*));
	for (i = 0; i < nbns; i++)
	    if (cur->prefix == ret[i]->prefix
		|| xmlStrEqual(cur->prefix, 0))
		break;

	if (i >= nbns) {
	    if (nbns >= maxns)
		return;
	    ret[nbns++] = cur;
	}
    }
}

static void
xsltParseStylesheetTemplate(struct xmlNode *template)
{
    struct xsltTemplate *ret;
    unsigned char *prop;

    ret = xsltNewTemplate();
    if (ret == 0)
	return;
    xsltGetInheritedNsList(ret, template);
    xsltGenericDebug();
    xsltGetQNameURI(&prop);
    xmlStrEqual(0, ret->name);
}

void xsltParseStylesheetTop(struct xmlNode *cur)
{
    xmlStrEqual(0, 0);

    while (cur != 0) {
	if (xmlStrEqual(cur->name, 0))
	    ;
	else if (xmlStrEqual(cur->name, 0))
	    ;
	else if (xmlStrEqual(cur->name, 0))
	    xsltParseStylesheetTemplate(cur);
    }
}

